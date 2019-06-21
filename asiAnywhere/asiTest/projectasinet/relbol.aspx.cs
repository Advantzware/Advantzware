
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web.UI.HtmlControls;
using System.Web.UI.WebControls.WebParts;
using System.Web;
using System.Text;
using System.IO;
using System.Web.UI;
#endregion

public partial class relbol : System.Web.UI.Page
{
    public Int32 seq = 0;
    private string strItem = "";

    protected void Page_Load(object sender, System.EventArgs e)
    {

        if (Request.QueryString["clean"] == "yes")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            SqlCommand cmd_insert = new SqlCommand("delete from relbol", conn);
            cmd_insert.ExecuteNonQuery();
            conn.Close();

            Response.Redirect("relbol.aspx");
        }


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];



        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "relbol.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;
            if (!Page.IsPostBack)
            {
                OutputLabel.Visible = false;
                HyperLink1.Visible = false;

                /*DateTextBox.Text = Convert.ToString(System.DateTime.Today.Date.ToShortDateString());
                CompanyTextBox.Text = PrmComp;            
                */

                //GridView1.Visible = false;

                RelQtyLabel.Text = "0";
                ScanQtyLabel.Text = "0";         
            }            

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

         
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = CompanyTextBox.Text.Trim();
            
           
                
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void addbutton_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);      
    }

    protected void insertseq(object sender, EventArgs e)
    {          
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {            
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            string cmd = "select max(seq) as seqnum from relbol";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            try
            {
                seq = Convert.ToInt32(ds.Tables[0].Rows[0][0]) + 1;
            }
            catch 
            {
                seq = 1;        
            }
            conn.Close();  

            TextBox Seq = (TextBox)FormView1.FindControl("vSeqTextBox");
            Seq.Text = Convert.ToString(seq);

            addbutton.Visible = false;
            updatebutton.Visible = false;
            deletebutton.Visible = false;
            printbolbutton.Visible = false;
          }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            addbutton.Visible = false;
            updatebutton.Visible = false;
            deletebutton.Visible = false;
            printbolbutton.Visible = false;
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            addbutton.Visible = true;
            updatebutton.Visible = true;
            deletebutton.Visible = true;
            printbolbutton.Visible = true;
        }
    }

    protected void updatebutton_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Edit);
    }

    protected void deletebutton_click(object sender, EventArgs e)
    {      
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();

        SqlCommand cmd_delete = new SqlCommand("delete from relbol where seq = '" + Session["relbol_seq_txt"] + "'", conn);
        cmd_delete.ExecuteNonQuery();
               

        string scancmd = "select sum(qty) as qty from relbol";
        SqlDataAdapter da1 = new SqlDataAdapter(scancmd, conn);
        DataSet ds1 = new DataSet();
        da1.Fill(ds1);

        string qty = Convert.ToString(ds1.Tables[0].Rows[0][0]);

        if (qty == "" || qty == "0")
        {
            ScanQtyLabel.Text = "0";
        }
        else
        {
            ScanQtyLabel.Text = qty;
        }


        conn.Close();

        /*FormView1.ChangeMode(FormViewMode.ReadOnly);    */       

        GridView1.DataBind();
        FormView1.DataBind();
         
    }

    protected void printbolbutton_click(object sender, EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Convert.ToInt32(GridView1.Rows.Count.ToString()) == 0)
        {
            return;
        }


        SqlConnection conn2 = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

        try
        {
            conn2.Open();
            SqlDataAdapter da = new SqlDataAdapter("select seq,release,tag,trailor,i_no,i_name,ord_no,job_no,job_no2,loc,loc_bin,cust_no,cases,qty_case,cases_unit,partial,qty,line,warned,po_no from relbol", conn2);                       
            DataSet ds = new DataSet();
            da.Fill(ds);


            StringBuilder str = new StringBuilder();

            /*str.Append("seq,release,tag,trailor,i_no,i_name,ord_no,job_no,job_no2,loc,loc_bin,cust_no,cases,qty_case,cases_unit,partial,qty,line,warned,po_no");
            str.AppendLine();
             */
            int n = ds.Tables[0].Columns.Count - 1;

            for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
            {
                for (int j = 0; j <= ds.Tables[0].Columns.Count - 1; j++)
                {
                    str.Append('"');
                    str.Append(ds.Tables[0].Rows[i][j].ToString());
                    str.Append('"');
                    for (int k = j; k < j + 1 && j < n; k++)
                    {
                        str.Append(",");
                    }
                }
                str.AppendFormat(Environment.NewLine);

            }
            //string filePath = @"D:\webapps\asinet\source\MyCSVFolder\Relbol.txt";
            string filePath = Server.MapPath("MyCSVFolder") + "\\" + UserLogin.UserName + "Relbol.txt";
            File.WriteAllText(filePath, str.ToString());

            SqlCommand cmd_delete = new SqlCommand("delete from relbol", conn2);
            cmd_delete.ExecuteNonQuery();

            conn2.Close();

            conn2.Dispose();

        }
        catch
        {
            conn2.Close();
        }
        finally
        {
            conn2.Close();
        }
        



        try
        {
            /*SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            string cmd = "select * from relbol where seq = " + Session["relbol_seq_txt"];
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            string seq = Convert.ToString(ds.Tables[0].Rows[0][0]);
            string release = Convert.ToString(ds.Tables[0].Rows[0][1]);
            string tag = Convert.ToString(ds.Tables[0].Rows[0][2]);
            string trailer = Convert.ToString(ds.Tables[0].Rows[0][3]);
            string inum = Convert.ToString(ds.Tables[0].Rows[0][4]);
            string iname = Convert.ToString(ds.Tables[0].Rows[0][5]);
            string ordno = Convert.ToString(ds.Tables[0].Rows[0][6]);
            string jobno = Convert.ToString(ds.Tables[0].Rows[0][7]);
            string jobno2 = Convert.ToString(ds.Tables[0].Rows[0][8]);
            string loc = Convert.ToString(ds.Tables[0].Rows[0][9]);
            string locbin = Convert.ToString(ds.Tables[0].Rows[0][10]);
            string custno = Convert.ToString(ds.Tables[0].Rows[0][11]);
            string cases = Convert.ToString(ds.Tables[0].Rows[0][12]);
            string qtycase = Convert.ToString(ds.Tables[0].Rows[0][13]);
            string casesunit = Convert.ToString(ds.Tables[0].Rows[0][14]);
            string partial = Convert.ToString(ds.Tables[0].Rows[0][15]);
            string qty = Convert.ToString(ds.Tables[0].Rows[0][16]);
            string tqty = Convert.ToString(ds.Tables[0].Rows[0][17]);
            string line = Convert.ToString(ds.Tables[0].Rows[0][18]);
            string warned = Convert.ToString(ds.Tables[0].Rows[0][19]);
            string ponum = Convert.ToString(ds.Tables[0].Rows[0][20]);

            conn.Close();
            */
            string filepath = Server.MapPath("MyCSVFolder") + "\\" + UserLogin.UserName + "Relbol.txt";

            bol bol = new bol();
            DataSet dsbol = new DataSet();
            //dsbol = bol.SelectRelBOL(UserLogin.UserName, "PrintBol", "", Convert.ToInt32(seq), Convert.ToInt32(release), tag, trailer, Convert.ToInt32(cases), inum, iname, Convert.ToInt32(ordno), Convert.ToInt32(qty), loc, locbin, custno, Convert.ToInt32(qtycase), Convert.ToInt32(casesunit), 0, jobno, Convert.ToInt32(jobno2), Convert.ToInt32(line), "no", ponum, 0, 0);
            dsbol = bol.SelectRelBOL(UserLogin.UserName, "PrintBol", "", 0, 0, "", "", 0, "", "", 0, 0, "", "", "", 0, 0, 0, "", 0, 0, "no", "", 0, 0,filepath);

            try
            {
                OutputLabel.Text = dsbol.Tables[0].Rows[0][0].ToString();
                string path = dsbol.Tables[0].Rows[0][0].ToString();
                HyperLink1.Text = path;

                string path2 = @"/pdfs/" + path;
                Session["print_create_rel_bol_list"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_create_rel_bol_list.aspx'); target='_blank'</script>");

                }
            }
            catch { }

            try
            {
                //SqlConnection conn1 = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                //conn1.Open();

                //SqlCommand cmd_delete = new SqlCommand("delete from relbol where release = '" + release + "'", conn1);
                //cmd_delete.ExecuteNonQuery();
                //conn1.Close();
            }
            catch { }

        }
        catch { }
        finally
        {
            GridView1.DataBind();
            FormView1.DataBind();
        }
        
    }    
       
    protected void InsertButtonClick(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Seq = (TextBox)FormView1.FindControl("vSeqTextBox");
        TextBox Release = (TextBox)FormView1.FindControl("vRelease_TextBox");
        TextBox Tag = (TextBox)FormView1.FindControl("vTag_TextBox");
        TextBox Trailer = (TextBox)FormView1.FindControl("vTrailerTextBox");
        TextBox Cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox Ino = (TextBox)FormView1.FindControl("vInoTextBox");
        TextBox Iname = (TextBox)FormView1.FindControl("vInameTextBox");
        TextBox OrdNo = (TextBox)FormView1.FindControl("vOrdNoTextBox");
        TextBox Qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox Loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox LocBin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox CustNo = (TextBox)FormView1.FindControl("vCustNoTextBox");
        TextBox QtyCase = (TextBox)FormView1.FindControl("vQtyCaseTextBox");
        TextBox CasesUnit = (TextBox)FormView1.FindControl("vCasesUnitTextBox");
        TextBox Partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        

        if (Seq.Text.Trim() == "")
            Seq.Text = Convert.ToString("0"); 
        if (Release.Text.Trim() == "")
            Release.Text = Convert.ToString("0");
        if (Cases.Text.Trim() == "")
            Cases.Text = Convert.ToString("0");
        if (OrdNo.Text.Trim() == "")
            OrdNo.Text = Convert.ToString("0");
        if (Qty.Text.Trim() == "")
            Qty.Text = Convert.ToString("0");
        if (QtyCase.Text.Trim() == "")
            QtyCase.Text = Convert.ToString("0");
        if (CasesUnit.Text.Trim() == "")
            CasesUnit.Text = Convert.ToString("0");
        if (Partial.Text.Trim() == "")
            Partial.Text = Convert.ToString("0");
                                                                                                                                                                

        bol bol = new bol();
        bool check = bol.CheckRelBOL(UserLogin.UserName, "RelInsert", "", Convert.ToInt32(Seq.Text.Trim()), Convert.ToInt32(Release.Text.Trim()), Tag.Text.Trim(), Trailer.Text.Trim(), Convert.ToInt32(Cases.Text.Trim()), Ino.Text.Trim(), Iname.Text.Trim(), Convert.ToInt32(OrdNo.Text.Trim()), Convert.ToInt32(Qty.Text.Trim()), Loc.Text.Trim(), LocBin.Text.Trim(), CustNo.Text.Trim(), Convert.ToInt32(QtyCase.Text.Trim()), Convert.ToInt32(CasesUnit.Text.Trim()), Convert.ToInt32(Partial.Text.Trim()), "", 0, 0, "", "", 0, 0,"");

        string value = Convert.ToString(check);

        if (value == "True")
        {                                    
            try
            {
                SqlConnection con1 = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                con1.Open();

                string strcmd2 = "select qty from relbol where release = '" + Convert.ToInt32(Release.Text.Trim()) + "' and ord_no = '" + Convert.ToInt32(OrdNo.Text.Trim()) + "' and warned='no'";
                SqlDataAdapter da3 = new SqlDataAdapter(strcmd2, con1);
                DataSet ds3 = new DataSet();
                da3.Fill(ds3);                

                if (ds3.Tables[0].Rows.Count < 1)
                {                                    
                    /*string cerror = "The scanned qty does not match the release qty.  Do you want to proceed?";

                    if (cerror != "" && (Convert.ToInt32(RelQtyLabel.Text.Trim())) != Convert.ToInt32(Qty.Text.Trim()))
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");
                        return;                           
                     }  
                     */

                    string cerror = "Qty scanned exceeds qty released for Order# ";

                    if (cerror != "" && (Convert.ToInt32(RelQtyLabel.Text.Trim())) < Convert.ToInt32(Qty.Text.Trim()))
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");
                        return;
                    }
                }

                int relqty = Convert.ToInt32(RelQtyLabel.Text.Trim());
                int totrelscanqty = Convert.ToInt32(ds3.Tables[0].Rows[0][0]) + Convert.ToInt32(Qty.Text.Trim());                

                if (totrelscanqty > relqty)
                {
                    string cerror = "Qty scanned exceeds qty released for Order# ";

                    if (cerror != "")
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");
                        return;
                    } 
                }
                else if (totrelscanqty != relqty)
                {
                    /*string cerror = "The scanned qty does not match the release qty.  Do you want to proceed?";

                    if (cerror != "")
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");
                        return;
                    }       
                     */
                }

                string strcmd = "select tag from relbol where tag = '" + Tag.Text.Trim() + "'";
                SqlDataAdapter da2 = new SqlDataAdapter(strcmd, con1);
                DataSet ds2 = new DataSet();
                da2.Fill(ds2);

                string tag = Convert.ToString(ds2.Tables[0].Rows[0][0]);

                if (tag == "")
                {
                    //Response.Write("hi");
                }
                else
                {
                    string cError = "Tag# already scanned...";

                    if (cError != "")
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
                    }

                    if (FormView1.CurrentMode == FormViewMode.Edit)
                    {
                        HiddenField TagHidden = (HiddenField)FormView1.FindControl("TagHiddenField");
                        Tag.Text = TagHidden.Value;
                    }

                    return;
                }

                con1.Close();
            }
            catch { }


            Int32 seq = 0;  

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            string cmd = "select max(seq) as seqnum from relbol";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            string seq1 = Convert.ToString(ds.Tables[0].Rows[0][0]);

            if (seq1 == "")
            {
                seq = 1;
            }
            else
            {
                seq = Convert.ToInt32((ds.Tables[0].Rows[0][0])) + 1;
            }

            SqlCommand cmd_insert = new SqlCommand("insert into relbol (seq, release, tag, trailor, i_no, i_name, ord_no, loc, loc_bin, cust_no, cases, qty_case, cases_unit, qty, job_no, job_no2,  po_no, line, rel_qty, warned) values ('" + seq + "','" + Release.Text + "','" + Tag.Text.Trim() + "','" + Trailer.Text.Trim() + "','" + Ino.Text.Trim() + "','" + Iname.Text.Trim() + "','" + OrdNo.Text + "','" + Loc.Text.Trim() + "','" + LocBin.Text.Trim() + "','" + CustNo.Text.Trim() + "','" + Cases.Text + "','" + QtyCase.Text + "','" + CasesUnit.Text + "','" + Qty.Text + "', '" + JobNoHiddenField.Value + "', '" + JobNo2HiddenField.Value + "', '" + PoNoHiddenField.Value + "', '" + LineHiddenField.Value + "', '" + RelQtyLabel.Text + "', 'no' )", conn);
            cmd_insert.ExecuteNonQuery();


            string scancmd = "select sum(qty) as qty from relbol where release = '" + Convert.ToInt32(Release.Text.Trim()) + "'";
            SqlDataAdapter da1 = new SqlDataAdapter(scancmd, conn);
            DataSet ds1 = new DataSet();
            da1.Fill(ds1);

            string qty = Convert.ToString(ds1.Tables[0].Rows[0][0]);

            if (qty == "" || qty == "0")
            {             
                ScanQtyLabel.Text = "0";
            }
            else
            {
                ScanQtyLabel.Text = qty;
            }


            conn.Close();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
       
        GridView1.DataBind();
       
    }


    protected void UpdateButtonClick(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Seq = (TextBox)FormView1.FindControl("vSeqTextBox");
        TextBox Release = (TextBox)FormView1.FindControl("vRelease_TextBox");
        TextBox Tag = (TextBox)FormView1.FindControl("vTag_TextBox");
        TextBox Trailer = (TextBox)FormView1.FindControl("vTrailerTextBox");
        TextBox Cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox Ino = (TextBox)FormView1.FindControl("vInoTextBox");
        TextBox Iname = (TextBox)FormView1.FindControl("vInameTextBox");
        TextBox OrdNo = (TextBox)FormView1.FindControl("vOrdNoTextBox");
        TextBox Qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox Loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox LocBin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox CustNo = (TextBox)FormView1.FindControl("vCustNoTextBox");
        TextBox QtyCase = (TextBox)FormView1.FindControl("vQtyCaseTextBox");
        TextBox CasesUnit = (TextBox)FormView1.FindControl("vCasesUnitTextBox");
        TextBox Partial = (TextBox)FormView1.FindControl("vPartialTextBox");

        if (Seq.Text.Trim() == "")
            Seq.Text = Convert.ToString("0");
        if (Release.Text.Trim() == "")
            Release.Text = Convert.ToString("0");
        if (Cases.Text.Trim() == "")
            Cases.Text = Convert.ToString("0");
        if (OrdNo.Text.Trim() == "")
            OrdNo.Text = Convert.ToString("0");
        if (Qty.Text.Trim() == "")
            Qty.Text = Convert.ToString("0");
        if (QtyCase.Text.Trim() == "")
            QtyCase.Text = Convert.ToString("0");
        if (CasesUnit.Text.Trim() == "")
            CasesUnit.Text = Convert.ToString("0");
        if (Partial.Text.Trim() == "")
            Partial.Text = Convert.ToString("0");


        bol bol = new bol();
        bool check = bol.CheckRelBOL(UserLogin.UserName, "RelInsert", "", Convert.ToInt32(Seq.Text.Trim()), Convert.ToInt32(Release.Text.Trim()), Tag.Text.Trim(), Trailer.Text.Trim(), Convert.ToInt32(Cases.Text.Trim()), Ino.Text.Trim(), Iname.Text.Trim(), Convert.ToInt32(OrdNo.Text.Trim()), Convert.ToInt32(Qty.Text.Trim()), Loc.Text.Trim(), LocBin.Text.Trim(), CustNo.Text.Trim(), Convert.ToInt32(QtyCase.Text.Trim()), Convert.ToInt32(CasesUnit.Text.Trim()), Convert.ToInt32(Partial.Text.Trim()), "", 0, 0, "", "", 0, 0,"");

        string value = Convert.ToString(check);        

        if (value == "True")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            SqlCommand cmd_update = new SqlCommand("update relbol set release = '" + Release.Text + "', tag = '" + Tag.Text.Trim() + "', trailor = '" + Trailer.Text.Trim() + "', i_no = '" + Ino.Text.Trim() + "', i_name = '" + Iname.Text.Trim() + "', ord_no = '" + OrdNo.Text + "', loc = '" + Loc.Text.Trim() + "', loc_bin = '" + LocBin.Text.Trim() + "', cust_no = '" + CustNo.Text.Trim() + "', cases = '" + Cases.Text + "', qty_case = '" + QtyCase.Text + "', cases_unit = '" + CasesUnit.Text + "', qty = '" + Qty.Text + "' where seq = '" + Seq.Text + "'", conn);            
            cmd_update.ExecuteNonQuery();
            conn.Close();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }       

        GridView1.DataBind();
    }



    protected void TagTextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Release = (TextBox)FormView1.FindControl("vRelease_TextBox");
        TextBox Tag = (TextBox)FormView1.FindControl("vTag_TextBox");
        TextBox Trailer = (TextBox)FormView1.FindControl("vTrailerTextBox");        
        TextBox OrderNo = (TextBox)FormView1.FindControl("vOrdNoTextBox");
        TextBox Units = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox ItemNo = (TextBox)FormView1.FindControl("vInoTextBox");
        TextBox CustNo = (TextBox)FormView1.FindControl("vCustNoTextBox");
        TextBox CasesUnit = (TextBox)FormView1.FindControl("vCasesUnitTextBox");
        TextBox Partial = (TextBox)FormView1.FindControl("vPartialTextBox");        

        TextBox ItemName = (TextBox)FormView1.FindControl("vInameTextBox");
        TextBox Loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox LocBin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox Qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox QtyCase = (TextBox)FormView1.FindControl("vQtyCaseTextBox");

        HiddenField hidden = (HiddenField)FormView1.FindControl("HiddenField1");

        try
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            string cmd = "select tag from relbol where tag = '" + Tag.Text.Trim() + "'";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            string tag = Convert.ToString(ds.Tables[0].Rows[0][0]);

            if (tag == "")
            {
                //Response.Write("hi");
            }
            else if (tag != "" && hidden.Value == "1") 
            {
                string cError = "Tag# already scanned...";

                if (cError != "")
                {
                    HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");           
                }

                if (FormView1.CurrentMode == FormViewMode.Edit)
                {
                    HiddenField TagHidden = (HiddenField)FormView1.FindControl("TagHiddenField");
                    Tag.Text = TagHidden.Value; 
                }
                   
                return;
            }

            conn.Close();
        }
        catch { }
        
                                       

        if (Release.Text.Trim() == "")
            Release.Text = Convert.ToString("0");

        try
        {
            LookUp lookup = new LookUp();
            DataSet dsTagLeave = new DataSet();

            dsTagLeave = lookup.SelectTagLookup("TagLeave", UserLogin.UserName, "", "", "", "", Convert.ToInt32(Release.Text.Trim()), Tag.Text.Trim());

            Tag.Text = dsTagLeave.Tables[0].Rows[0][0].ToString();
            ItemNo.Text = dsTagLeave.Tables[0].Rows[0][1].ToString();
            ItemName.Text = dsTagLeave.Tables[0].Rows[0][2].ToString();
            JobNoHiddenField.Value = dsTagLeave.Tables[0].Rows[0][3].ToString();
            JobNo2HiddenField.Value = dsTagLeave.Tables[0].Rows[0][4].ToString();
            Loc.Text = dsTagLeave.Tables[0].Rows[0][5].ToString();
            LocBin.Text = dsTagLeave.Tables[0].Rows[0][6].ToString();
            OrderNo.Text = dsTagLeave.Tables[0].Rows[0][7].ToString();
            PoNoHiddenField.Value = dsTagLeave.Tables[0].Rows[0][8].ToString();
            Qty.Text = dsTagLeave.Tables[0].Rows[0][9].ToString();
            QtyCase.Text = dsTagLeave.Tables[0].Rows[0][10].ToString();
            LineHiddenField.Value = dsTagLeave.Tables[0].Rows[0][12].ToString();
            Units.Text = dsTagLeave.Tables[0].Rows[0][13].ToString();
            CustNo.Text = dsTagLeave.Tables[0].Rows[0][14].ToString();
            CasesUnit.Text = dsTagLeave.Tables[0].Rows[0][15].ToString();
            Partial.Text = dsTagLeave.Tables[0].Rows[0][16].ToString();

            Trailer.Focus();
        }
        catch { }

    }

    protected void TrailerTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Release = (TextBox)FormView1.FindControl("vRelease_TextBox");
        TextBox Trailer = (TextBox)FormView1.FindControl("vTrailerTextBox");

        LookUp lookup = new LookUp();
        lookup.SelectTrailerLookup("TrailerLeave", UserLogin.UserName, "", "", "", "", Trailer.Text.Trim(), Convert.ToInt32(Release.Text.Trim()));
    }

    protected void ReleaseTextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Release = (TextBox)FormView1.FindControl("vRelease_TextBox");
        TextBox Tag = (TextBox)FormView1.FindControl("vTag_TextBox");
        

        Int32 RelQty = 0;
        string release = "";


        try
        {
            Int32 rel = Convert.ToInt32(Release.Text);
        }
        catch {
            HttpContext.Current.Response.Write("<script>alert('Enter Numeric Value!')</script>");
            Release.Text = "";
            Release.Focus();
            return;
        }

        try
        {
            LookUp lookup = new LookUp();
            DataSet dsReleaseLook = new DataSet();

            dsReleaseLook = lookup.SelectReleaseLookup("ReleaseLeave", UserLogin.UserName, "", "", "", "", Convert.ToInt32(Release.Text.Trim()));

            if (dsReleaseLook.Tables[0].Rows.Count == 0)
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid Release!')</script>");
                Release.Text = "";
                Release.Focus();
            }
            else
            {
                RelQtyLabel.Text = dsReleaseLook.Tables[0].Rows[0][6].ToString();
                Tag.Focus();
            }            
        }
        catch { }
        
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {        
        Session["relbol_seq_txt"] = GridView1.SelectedRow.Cells[1].Text;        
    }

}
