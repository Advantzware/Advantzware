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

public partial class ArInvoice : System.Web.UI.Page
{
    string raw_url;
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";

    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();
            
        }
        catch { }

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
                
            }
            conn.Close();
        }
        catch { conn.Close(); }



        if (!Page.IsPostBack)
        {
            CheckBox1.Checked = true;
            CheckBox2.Checked = false;
            
            BuildDataSource();
        }
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;

        
        if (Session["User"] != null)
        {
            lblUser.Text = UserLogin.UserName;

            string vUserId = UserLogin.UserName;
            string vPage = "Arinvoice.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                customerlabel.Visible = false;
                txt_cust.Visible = false;
                CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");


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
            //Response.Write(PrmCompArviewinvoice);
            if (vCanRunArviewinvoice == true)
            {
                lnkviewinvoice.Visible = true;

            }

            if (vCanRunArviewinvoice == false)
            {
                lnkviewinvoice.Visible = false;

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
            string vUserIdArbol = UserLogin.UserName;
            string vPageArbol = "Arbol.aspx";
            string aUsersArbol = null;
            string PrmCompArbol = null;
            bool vCanCreateArbol = false;
            bool vCanRunArbol = false;
            bool vCanUpdateArbol = false;
            bool vCanDeleteArbol = false;

            func1 f1Arbol = new func1();
            //Response.Write(Page);
            f1Arbol.CheckProgramPermissions(vPageArbol, vUserIdArbol, ref  vCanCreateArbol, ref  vCanRunArbol, ref  vCanUpdateArbol, ref  vCanDeleteArbol, ref  PrmCompArbol, ref  aUsersArbol);

            lblComp.Text = PrmCompArbol;
            //Response.Write(vCanRun);
            if (vCanRunArbol == true)
            {
                lnkbol.Visible = true;

            }

            if (vCanRunArbol == false)
            {
                lnkbol.Visible = false;

            }

        }

        lbl_page.Text = "ArInvoice";
       
        GridView2.SelectedIndex = Convert.ToInt32(Session["Arindex"]);
        Session["index4"] = Session["line"];
        try
        {
            if (Session["Arindex"] == null)
            {
                
                GridView2.SelectedIndex = 0;
                Session["brwsinvoice"] = ((Label)GridView2.SelectedRow.FindControl("Order_Label")).Text;
                Session["vBol"] = ((Label)GridView2.SelectedRow.FindControl("bol_Label")).Text;
                Session["vCust"] = ((Label)GridView2.SelectedRow.FindControl("cust_Label")).Text;
                Session["ar_inv_cust_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckey_Label")).Text;
                Session["ar_inv_view_inv"] = ((Label)GridView2.SelectedRow.FindControl("inv_Label")).Text;
                
            }
            if (Session["Arindex"] != null)
            {
                Session["brwsinvoice"] = ((Label)GridView2.SelectedRow.FindControl("Order_Label")).Text;
                Session["vBol"] = ((Label)GridView2.SelectedRow.FindControl("bol_Label")).Text;
                Session["vCust"] = ((Label)GridView2.SelectedRow.FindControl("cust_Label")).Text;
                Session["ar_inv_cust_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckey_Label")).Text;
                Session["ar_inv_view_inv"] = ((Label)GridView2.SelectedRow.FindControl("inv_Label")).Text;
            }

        }
        catch
        {
           
        }

        if (CheckBox1.Checked == true)
        {
            openinvoice.Value = "YES";
        }
        else
        {
            openinvoice.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            paidinvoice.Value = "YES";
        }
        else
        {
            paidinvoice.Value = "NO";
        }


        try
        {
           
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            
            GridView2.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            
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

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        
        UserClass UserLogin = (UserClass)Session["User"];


        if (CheckBox1.Checked == true)
        {
            openinvoice.Value = "YES";
        }
        else
        {
            openinvoice.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            paidinvoice.Value = "YES";
        }
        else
        {
            paidinvoice.Value = "NO";
        }
        
        Session["prmAction2"] = "Search";
        Session["prmInvoice"] = txt_inv.Text.Trim();
        Session["prmCustomer"] = txt_cust.Text.Trim();
        Session["prmItem"] = txt_item.Text.Trim();
        Session["prmPart"] = txt_part.Text.Trim();
        Session["prmCustPo"] = txt_po.Text.Trim();
        Session["prmBOL"] = txt_bol.Text.Trim();
        Session["prmEstimate"] = txt_est.Text.Trim();
        Session["prmDate"] = txt_date.Text.Trim();
        Session["prmOpen"] = openinvoice.Value.Trim();
        Session["prmPaid"] = paidinvoice.Value.Trim();

        Session["Arindex"] = null;
        //Session["index2"] = null;
        //Session["item"] = null;
        //Session["line"] = null;
        //Session["index3"] = null;
        //Session["index4"] = null;
       Page_Load(sender, e);
    }



    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arviewinvoice.aspx");
    }

    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Session["brwsinvoice"] = Session["brwsinvoice"];
        Response.Redirect("Arinvoice.aspx");
    }


    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";

        txt_inv.Text = str.ToString();
        txt_cust.Text = str.ToString();
        txt_item.Text = str.ToString();

        txt_part.Text = str.ToString();
        txt_po.Text = str.ToString();
        txt_bol.Text = str.ToString();
        txt_est.Text = str.ToString();
        txt_date.Text = str.ToString();
        ddl_order.Text = str.ToString();
        
        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmAction2"] = "select";
        Session["prmInvoice"] = txt_inv.Text.Trim();
        Session["prmCustomer"] = txt_cust.Text.Trim();
        Session["prmItem"] = txt_item.Text.Trim();
        Session["prmPart"] = txt_part.Text.Trim();
        Session["prmCustPo"] = txt_po.Text.Trim();
        Session["prmBOL"] = txt_bol.Text.Trim();
        Session["prmEstimate"] = txt_est.Text.Trim();
        Session["prmDate"] = txt_date.Text.Trim();
        Session["prmOpen"] = openinvoice.Value.Trim();
        Session["prmPaid"] = paidinvoice.Value.Trim();

        Session["Arindex"] = null;
        Session["ar_inv_cust_rec_key"] = null;
        Session["arinvoice_gridview_sort"] = null;
        
        //Session["item"] = null;
        //Session["line"] = null;
        //Session["index3"] = null;
        //Session["index4"] = null;      
        Page_Load(sender, e);
    }
    protected void lnkcreditstatus_Click(object sender, EventArgs e)
    {
        Session["brwsinvoice"] = Session["brwsinvoice"];
        Session["vCust"] = Session["vCust"];
        Response.Redirect("Arcreditstatusinv.aspx");
    }
    protected void lnkbol_Click(object sender, EventArgs e)
    {

        Session["brwsinvoice"] = Session["brwsinvoice"];
        Session["vBol"] = Session["vBol"];
        Response.Redirect("Arbol.aspx");

    }
   

    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }

    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["prmInvoice"] == null || Convert.ToString(Session["prmInvoice"]) == "")
            Session["prmInvoice"] = "";

        if (Session["prmCustomer"] == null || Convert.ToString(Session["prmCustomer"]) == "")
            Session["prmCustomer"] = "";
        if (Session["prmItem"] == null || Convert.ToString(Session["prmItem"]) == "")
            Session["prmItem"] = "";
        if (Session["prmPart"] == null || Convert.ToString(Session["prmPart"]) == "")
            Session["prmPart"] = "";
        if (Session["prmCustPo"] == null || Convert.ToString(Session["prmCustPo"]) == "")
            Session["prmCustPo"] = "";
        if (Session["prmBOL"] == null || Convert.ToString(Session["prmBOL"]) == "")
            Session["prmBOL"] = "";
        if (Session["prmEstimate"] == null || Convert.ToString(Session["prmEstimate"]) == "")
            Session["prmEstimate"] = "";
        if (Session["prmDate"] == null || Convert.ToString(Session["prmDate"]) == "")
            Session["prmDate"] = "";
        if (Session["prmOpen"] == null || Convert.ToString(Session["prmOpen"]) == "")
            Session["prmOpen"] = "";
        if (Session["prmPaid"] == null || Convert.ToString(Session["prmPaid"]) == "")
            Session["prmPaid"] = "";

        if (Convert.ToString(Session["prmAction2"]) != "Search" || Convert.ToString(Convert.ToString(Session["prmAction2"])) == "" || Session["prmAction2"] == null)
        {
            Session["prmAction2"] = "Select";
        }
       
            browsinvoice ar_inv = new browsinvoice();
            DataSet ds_ar_inv = new DataSet();
            ds_ar_inv = ar_inv.ArInvoice(Convert.ToString(Session["prmAction2"]), UserLogin.UserName, "", Convert.ToString(Session["prmInvoice"]), Convert.ToString(Session["prmCustomer"]), Convert.ToString(Session["prmItem"]), Convert.ToString(Session["prmPart"]), Convert.ToString(Session["prmCustPo"]), Convert.ToString(Session["prmBOL"]), Convert.ToString(Session["prmEstimate"]), Convert.ToString(Session["prmDate"]), Convert.ToString(Session["prmOpen"]), Convert.ToString(Session["prmPaid"]));
            //Response.Write(ds_ar_inv.Tables[0].Rows.Count);
        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "Arinvoice.aspx" + "' and sub_program = '" + "Arinvoice.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].Rows.Count);
        
            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "Arinvoice.aspx" + "' and sub_program = '" + "Arinvoice.aspx" + "' and display = 0 ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);
            //Response.Write(ds2.Tables[0].Rows.Count);
            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["initial_val_arin"] = ds2.Tables[0].Rows.Count;
                for (int j = 0; j < ds2.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());
                    //Response.Write(fld + "," +seq);
                    if (ds2.Tables[0].Rows[0][0].ToString() == "0")
                    {
                        ds_ar_inv.Tables[0].Columns[fld].SetOrdinal(seq - 1);
                    }

                }
            }
            else
            {
                Session["initial_val_arin"] = null;
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "Arinvoice.aspx" + "' and sub_program = '" + "Arinvoice.aspx" + "' and display = 0  order by seq_no ";                
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val_arin"] = mainds.Tables[0].Rows.Count; ;
               
                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                   // Response.Write(fld + "," + seq);
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_ar_inv.Tables[0].Columns[fld].SetOrdinal(seq-1);
                    }
                }


            }

                for (int i = 0; i < ds_ar_inv.Tables[0].Columns.Count; i++)
                {
                    //Response.Write(",");
                    //Response.Write(ds_ar_inv.Tables[0].Columns[i].ColumnName.ToString());
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arinv-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Inv#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arbol-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Bol#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arcust-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Cust#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arinv-date")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Inv Date";
                    }

                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Ari-no")
                    {                        
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "FG Item#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arpart-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Cust Part#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arord-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Order#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arpo-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Cust Po#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arest-no")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Est#";
                    }
                    if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Arname")
                    {
                        ds_ar_inv.Tables[0].Columns[i].ColumnName = "Name";
                    }
                }

                
                DataView dv = ds_ar_inv.Tables[0].DefaultView;
                
                if (Session["arinvoice_gridview_sort"] == null)
                {
                    dv.Sort = "FG Item#    DESC";
                }
                if (Session["arinvoice_gridview_sort"] != null)
                {
                    dv.Sort = Convert.ToString(Session["arinvoice_gridview_sort"]);
                }

                GridView2.DataSource = dv;
                GridView2.DataBind();




            }
            catch { }
    }
    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {

            if (Session["initial_val_arin"] != null)
            {

                for (int show = Convert.ToInt32(Session["initial_val_arin"]) + 6; show < 17; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                e.Row.Cells[17].Visible = false;
                e.Row.Cells[18].Visible = false;
                e.Row.Cells[19].Visible = false;
            }
            if (Session["initial_val_arin"] == null)
            {
                e.Row.Cells[17].Visible = false;
                e.Row.Cells[18].Visible = false;
                e.Row.Cells[19].Visible = false;
            }
        }
        catch { }
    }
    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        
        GridView2.PageIndex = e.NewPageIndex;        
        Session["ar_inv_page_index"] = e.NewPageIndex;
        if (Convert.ToInt32(Session["ar_inv_page_index"]) == 0)
        {
            Response.Write("<script>window.location.href='Arinvoice.aspx';</script>");
        }
       
    }
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {

        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {

                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Inv Date"))
                {
                  //  Response.Write(e.Row.Cells[i + 6].Text.ToString());
                    string[] val = e.Row.Cells[i + 6].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 6].Text = val[0].ToString();
                }
            }
        }
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }
    }
    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {

            Session["Arindex"] = GridView2.SelectedIndex;
            Session["brwsinvoice"] = ((Label)GridView2.SelectedRow.FindControl("Order_Label")).Text;
            Session["vBol"] = ((Label)GridView2.SelectedRow.FindControl("bol_Label")).Text;
            Session["vCust"] = ((Label)GridView2.SelectedRow.FindControl("cust_Label")).Text;
            Session["ar_inv_cust_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckey_Label")).Text;
            Session["ar_inv_view_inv"] = ((Label)GridView2.SelectedRow.FindControl("inv_Label")).Text;

        }
        catch { }
       
    }
    public SortDirection GridViewSortDirection
    {
        get
        {
            if (ViewState["sortDirection"] == null)
                ViewState["sortDirection"] = SortDirection.Ascending;
            return (SortDirection)ViewState["sortDirection"];
        }
        set
        {
            ViewState["sortDirection"] = value;
        }
    }
    protected void GridView2_Sorting(object sender, GridViewSortEventArgs e)
    {
        string sortExpression = e.SortExpression;
        if (GridViewSortDirection == SortDirection.Ascending)
        {
            GridViewSortDirection = SortDirection.Descending;
            SortGridView(sortExpression, " DESC");
        }
        else
        {
            GridViewSortDirection = SortDirection.Ascending;
            SortGridView(sortExpression, " ASC");
        }
    }
    private void SortGridView(string sortExpression, string direction)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

       Session["arinvoice_gridview_sort"] = sortExpression + direction;

            
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

    

}
