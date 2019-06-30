using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Linq;

public partial class ar_bol_inv : System.Web.UI.Page
{
    string raw_url;

    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        UserClass.CheckLogin(Page);
        lbl_page.Text = "BOL";
        
        try
        {
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });            
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();
        }
        catch { }
        if (!Page.IsPostBack)
        {
            UserClass.CheckLogin(Page);
            
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;

                string vUserId = UserLogin.UserName;
                string vPage = "Arbol.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }

                string vUserIdArinvoice = UserLogin.UserName;
                string vPageArinvoice = "Arinvoice.aspx";
                string aUsersArinvoice = null;
                string PrmCompArinvoice = null;
                bool vCanCreateArinvoice = false;
                bool vCanRunArinvoice = false;
                bool vCanUpdateArinvoice = false;
                bool vCanDeleteArinvoice = false;

                func1 f1Arinvoice = new func1();
                //Response.Write(Page);
                f1Arinvoice.CheckProgramPermissions(vPageArinvoice, vUserIdArinvoice, ref  vCanCreateArinvoice, ref  vCanRunArinvoice, ref  vCanUpdateArinvoice, ref  vCanDeleteArinvoice, ref  PrmCompArinvoice, ref  aUsersArinvoice);

                lblComp.Text = PrmCompArinvoice;
                //Response.Write(PrmCompArinvoice);
                if (vCanRunArinvoice == true)
                {
                    lnkbrowsinvoice.Visible = true;

                }

                if (vCanRunArinvoice == false)
                {
                    lnkbrowsinvoice.Visible = false;

                }

                string vUserIdArcreditstatusinv = UserLogin.UserName;
                string vPageArcreditstatusinv = "Arcreditstatusinv.aspx";
                string aUsersArcreditstatusinv = null;
                string PrmCompArcreditstatusinv = null;
                bool vCanCreateArcreditstatusinv = false;
                bool vCanRunArcreditstatusinv = false;
                bool vCanUpdateArcreditstatusinv = false;
                bool vCanDeleteArcreditstatusinv = false;

                func1 f1Arcreditstatusinv = new func1();
                //Response.Write(Page);
                f1Arcreditstatusinv.CheckProgramPermissions(vPageArcreditstatusinv, vUserIdArcreditstatusinv, ref  vCanCreateArcreditstatusinv, ref  vCanRunArcreditstatusinv, ref  vCanUpdateArcreditstatusinv, ref  vCanDeleteArcreditstatusinv, ref  PrmCompArcreditstatusinv, ref  aUsersArcreditstatusinv);

                lblComp.Text = PrmCompArcreditstatusinv;
                //Response.Write(vCanRun);
                if (vCanRunArcreditstatusinv == true)
                {
                    lnkcreditstatus.Visible = true;

                }

                if (vCanRunArcreditstatusinv == false)
                {
                    lnkcreditstatus.Visible = false;

                }
                string vUserIdArviewinvoice = UserLogin.UserName;
                string vPageArviewinvoice = "Arviewinvoice.aspx";
                string aUsersArviewinvoice = null;
                string PrmCompArviewinvoice = null;
                bool vCanCreateArviewinvoice = false;
                bool vCanRunArviewinvoice = false;
                bool vCanUpdateArviewinvoice = false;
                bool vCanDeleteArviewinvoice = false;

                func1 f1Arviewinvoice = new func1();
                //Response.Write(Page);
                f1Arviewinvoice.CheckProgramPermissions(vPageArviewinvoice, vUserIdArviewinvoice, ref  vCanCreateArviewinvoice, ref  vCanRunArviewinvoice, ref  vCanUpdateArviewinvoice, ref  vCanDeleteArviewinvoice, ref  PrmCompArviewinvoice, ref  aUsersArviewinvoice);

                lblComp.Text = PrmCompArviewinvoice;
                //Response.Write(vCanRun);
                if (vCanRunArviewinvoice == true)
                {
                    lnkviewinvoice.Visible = true;

                }

                if (vCanRunArviewinvoice == false)
                {
                    lnkviewinvoice.Visible = false;

                }

            }
        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from button_maintain where parent = 'Arinvoice.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5,chk1,chk2,chk3,chk4,chk5,user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,user11,user12,user13,user14,user15) values ('Arinvoice.aspx','AR Invoice Inquiry','Credit Status','Bol','Print Bol','Signed Bol','Print Invoice','True','True','True','True','True','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }

            foreach (DataRow dr in ds.Tables[0].Rows)
            {
                string[] ss1 = dr["user1"].ToString().Split(',');
                string[] ss2 = dr["user2"].ToString().Split(',');
                string[] ss3 = dr["user3"].ToString().Split(',');
                string[] ss4 = dr["user4"].ToString().Split(',');
                string[] ss5 = dr["user5"].ToString().Split(',');


                if (ss1.Contains(UserLogin.UserName) || ss1.Contains("*"))
                {                   
                    if (dr["chk1"].ToString() == "False")
                        liCreditStatus.Attributes.Add("style", "display:none");
                }
                if (ss2.Contains(UserLogin.UserName) || ss2.Contains("*"))
                {                    
                    if (dr["chk2"].ToString() == "False")
                        liBol.Attributes.Add("style", "display:none");
                }
                if (ss3.Contains(UserLogin.UserName) || ss3.Contains("*"))
                {                    
                    if (dr["chk3"].ToString() == "False")
                        liPrintBol.Attributes.Add("style", "display:none");
                }
                if (ss4.Contains(UserLogin.UserName) || ss4.Contains("*"))
                {                   
                    if (dr["chk4"].ToString() == "False")
                        liPrintSigned.Attributes.Add("style", "display:none");
                }
               


            }
            conn.Close();
        }
        catch { conn.Close(); }
    }
    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arinvoice.aspx");
    }
    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arviewinvoice.aspx");
    }
    protected void lnkcreditstatus_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arcreditstatusinv.aspx");
    }
    protected void bolButton(object sender, EventArgs e)
    {
        string swhere1 = Request.Form["selectradio"];
        if (swhere1 != null)
        {
            //Response.Write("hello");
            Session["prmAction"] = "printBol";
            Session["vRowid"] = swhere1;
            if (!Request.Browser.Browser.Contains("Safari"))
                Response.Write("<script>window.open('bolInvPrint.aspx'); target='_blank'</script>");
            else
                Response.Redirect("bolInvPrint.aspx");
        }
        else
        {
            Response.Write("<script>alert('There has not been an BOL created for this order yet')</script>");
        }
    }
    protected void SignbolButton(object sender, EventArgs e)
    {
        string swhere1 = Request.Form["selectradio"];
        if (swhere1 != null)
        {
            //Response.Write("hello");
            Session["prmAction"] = "SignBol";
            Session["vRowid"] = swhere1;
            if (!Request.Browser.Browser.Contains("Safari"))
                Response.Write("<script>window.open('SignBolPrint.aspx'); target='_blank'</script>");
            else
                Response.Redirect("SignBolPrint.aspx");
        }
        else
        {
            Response.Write("<script>alert('There has not been an BOL created for this order yet')</script>");
        }
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["brwsinvoice"] == null || Convert.ToString(Session["brwsinvoice"]) == "")
            Session["brwsinvoice"] = "";

        if (Session["vBol"] == null || Convert.ToString(Session["vBol"]) == "")
            Session["vBol"] = "";        

        browsinvoice br_inv = new browsinvoice();
        DataSet ds_br_inv = new DataSet();
        ds_br_inv = br_inv.BolInv(UserLogin.UserName, "", Convert.ToString(Session["brwsinvoice"]), Convert.ToString(Session["vBol"]));
        //Response.Write(ds_br_inv.Tables[0].Rows.Count);
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select col_val,col_seq,display  from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "Arbol.aspx" + "' and sub_program = '" + "Arbol.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count > 0)
            {
                Session["initial_val_bol"] = ds.Tables[0].Rows.Count;
                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                   // Response.Write(fld + "," +seq);
                    if (ds.Tables[0].Rows[j][2].ToString() == "0" )
                    {
                        ds_br_inv.Tables[0].Columns[fld].SetOrdinal(seq-1);
                    }
                }
            }
            else
            {
                
            string cmd_main = "select * from column_maintenance  where  main_program = '" + "Arbol.aspx" + "' and sub_program = '" + "Arbol.aspx" + "' order by seq_no ";
            //Response.Write(cmd_main.ToString());
            SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
            DataSet mainds = new DataSet();
            mainda.Fill(mainds);
            Session["initial_val_bol"] = null;
            for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][5].ToString().Trim().ToString());
                   // Response.Write(fld + "," +seq);
                    if (mainds.Tables[0].Rows[j][6].ToString() == "0" )
                    {
                        ds_br_inv.Tables[0].Columns[fld].SetOrdinal(seq-1);
                    }
                }

            }

            for (int i = 0; i < ds_br_inv.Tables[0].Columns.Count; i++)
            {
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolNo")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Bol#";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolIno")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Item#";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolTag")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Tag";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolJobNo")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Job#";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolinNo2")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = " ";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolLoc")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Whse";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolLocbin")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Location";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolCases")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Units";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolQtycase")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Qty/Unit";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolPartial")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Partial";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vTotal")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Total";
                }
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolweight")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Weight";
                }                
                if (ds_br_inv.Tables[0].Columns[i].ColumnName == "vBolScode")
                {
                    ds_br_inv.Tables[0].Columns[i].ColumnName = "Ship Status";
                }
            }

            DataView dv = ds_br_inv.Tables[0].DefaultView;
            //dv.Sort = "FG Item# DESC";

            GridView2.DataSource = dv;
            GridView2.DataBind();
        }
        catch { }
    }
    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Session["initial_val_bol"] != null)
            {
               // Response.Write(Session["initial_val_bol"]);
                //e.Row.Cells[1].Visible = false;
                for (int show = Convert.ToInt32(Session["initial_val_bol"]) + 1; show < 15; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                
            }
            if (Session["initial_val_bol"] == null)
            {
                e.Row.Cells[14].Visible = false;
            }

        }
        catch { }
    }
    
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {       
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }
    }
    
    
    protected void GridView2_Sorting(object sender, GridViewSortEventArgs e)
    {   
    
    }

    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView2.PageIndex = e.NewPageIndex;
        Session["ar_inv_bol_page_index"] = e.NewPageIndex;
        if (Convert.ToInt32(Session["ar_inv_bol_page_index"]) == 0)
        {
            Response.Write("<script>window.location.href='Arbol.aspx';</script>");
        }

    }
    
}
