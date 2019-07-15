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
public partial class itemhistoryfile : System.Web.UI.Page
{
   public static string prmAction = "Select";
   public static string prmOrderNum = "";
   public static string prmItemNum = "";
   public static string prmJob = "";
   public static string prmJob2 = "";
   public static string prmCode = "";
   public static string prmDate = "";
   public static string prmTag = "";
   public static string prmWareHouse = "";
   public static string prmPoNo = "";
    string raw_url;
   
    protected void Page_PreRender(object sender, EventArgs e)
    {
        prmItemNum = Convert.ToString(Session["item_list_item"]);
        BuildDataSource();
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        raw_url = Page.Request.RawUrl;
        string[] url = raw_url.Split(new char[] { '/' });
        Session["main_prgrm_url"] = url[2].ToString();
        Session["sub_prgrm_url"] = url[2].ToString();

        if (Session["Item_Inquiry_value_check"] != null)
        {
            LinkButton1.Visible = false;
            LinkButton2.Visible = false;
            LinkButton3.Visible = false;
        }
        else
        {
            //ImageButton iteminquiry = (ImageButton)Master.FindControl("listitem");
            //iteminquiry.Visible = false;
            HtmlGenericControl iteminquiry = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            iteminquiry.Attributes.Add("style", "display:none");
            if (Session["view_order_entry_pages"] == null)
            {
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages"] != null)
            {
                LinkButton1.Visible = false;
            }
            if (Session["view_order_entry_pages_with_estimate"] != null)
            {
                LinkButton1.Visible = false;
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                LinkButton3.Visible = false;
            }
        }

        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "itemhistory.aspx";
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
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        try
        {
            txt_fgitem.Text = Session["item"].ToString();            
            Label name = (Label)Master.FindControl("lbl_page");
            name.Text = "History";
            //ImageButton history = (ImageButton)Master.FindControl("history");
            //history.ImageUrl = "~/img/history1.jpg";
        }
        catch { }
        //if (!Page.IsPostBack)        

        //}

        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");

            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
           
        }
        catch
        {
            //return;
        }

    }

    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

       
        if (prmAction != "Search" || prmAction == "" )
        {
            prmAction = "Select";
        }
        
        itemhistory ar_inv = new itemhistory();
        DataSet ds_ar_inv = new DataSet();
        ds_ar_inv = ar_inv.SelectHistory(UserLogin.UserName, prmAction, prmOrderNum, Convert.ToString(Session["item_list_item"]), prmJob, prmJob2, prmCode, prmDate, prmTag, prmWareHouse, prmPoNo);
        //Response.Write(ds_ar_inv.Tables[0].Rows.Count);

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "itemhistory.aspx" + "' and sub_program = '" + "itemhistory.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].Rows.Count);

            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "itemhistory.aspx" + "' and sub_program = '" + "itemhistory.aspx" + "' and display = 0 ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);
            //Response.Write(ds2.Tables[0].Rows.Count);
            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["initial_val_itemhistory"] = ds2.Tables[0].Rows.Count;
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
                Session["initial_val_itemhistory"] = null;
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "itemhistory.aspx" + "' and sub_program = '" + "itemhistory.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val_itemhistory"] = mainds.Tables[0].Rows.Count; ;

                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                    // Response.Write(fld + "," + seq);
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_ar_inv.Tables[0].Columns[fld].SetOrdinal(seq - 1);
                    }
                }


            }

            ds_ar_inv.Tables[0].Columns["cRecKey"].SetOrdinal(17);
            ds_ar_inv.Tables[0].Columns["q-avail"].SetOrdinal(16);
            ds_ar_inv.Tables[0].Columns["i-name"].SetOrdinal(18);
            ds_ar_inv.Tables[0].Columns["q-onh"].SetOrdinal(19);
            for (int i = 0; i < ds_ar_inv.Tables[0].Columns.Count; i++)
            {               
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "TDate")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "TR Date";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "RCode")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "TR Code";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "ItemNum")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "FG Item #";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Tag")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Tag #";
                }

                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Po-no")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "PO #";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "job-no")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Job #";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "job-no2")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Job2 #";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Loc")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Whse";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Cust")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Cust #";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Loc-Bin")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Bin";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Qty-Case")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Qty / Case";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Unit")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Units / Pallet";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "QtyPlt")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Qty / Pallet";
                }

                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "Pallet")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Pallets";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "qty")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Quantity";
                }
                if (ds_ar_inv.Tables[0].Columns[i].ColumnName == "usr")
                {
                    ds_ar_inv.Tables[0].Columns[i].ColumnName = "Userid";
                }
            }


            DataView dv = ds_ar_inv.Tables[0].DefaultView;

            if (Session["arinvoice_gridview_sort"] == null)
            {
                dv.Sort = "TR Date  DESC";
            }
            if (Session["arinvoice_gridview_sort"] != null)
            {
                dv.Sort = Convert.ToString(Session["arinvoice_gridview_sort"]);
            }

            GridView1.DataSource = dv;
            GridView1.DataBind();




        }
        catch { }
    }
    protected void btn_go_Click(object sender, EventArgs e)
    {
        prmAction = "Search";
        prmOrderNum = "";

        prmJob = txt_job1.Text;
        prmJob2 = txt_job2.Text;
        prmCode = RCodeTextBox.Text;
        prmDate = txt_date.Text;
        prmTag = txt_tag.Text;
        prmWareHouse = txt_ware.Text;
        prmPoNo = txt_pono.Text;
        

    }
    protected void btn_showall_Click(object sender, EventArgs e)
    {
        {
            string str = "";
            string str1 = "B";

            txt_job1.Text = str.ToString();
            txt_job2.Text = str.ToString();
            RCodeTextBox.Text = str1.ToString();
            txt_date.Text = str.ToString();
            txt_tag.Text = str.ToString();
            txt_ware.Text = str.ToString();
            txt_pono.Text = str.ToString();

            prmAction = "Select";
            prmOrderNum = "";
            prmItemNum = "";
            prmJob = "";
            prmJob2 = "";
            prmCode = "";
            prmDate = "";
            prmTag = "";
            prmWareHouse = "";
            prmPoNo = "";
           


        }
    }
    protected void btn_fgitem_Click(object sender, EventArgs e)
    {
        txt_tag.Text = Session["item"].ToString();
    }
    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }
    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            Session["gridsize"] = ddl_display.Text;
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            ObjectDataSource3.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        }
        catch { }

    }

    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {

            if (Session["initial_val_itemhistory"] != null)
            {

                for (int show = Convert.ToInt32(Session["initial_val_itemhistory"]) + 1; show < 16; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                
                e.Row.Cells[17].Visible = false;
                e.Row.Cells[18].Visible = false;
                e.Row.Cells[19].Visible = false;
                e.Row.Cells[20].Visible = false;
                e.Row.Cells[21].Visible = false;
                e.Row.Cells[22].Visible = false;
            }
            if (Session["initial_val_itemhistory"] == null)
            {
                e.Row.Cells[17].Visible = false;
                e.Row.Cells[18].Visible = false;
                e.Row.Cells[19].Visible = false;
            }
        }
        catch { }
    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView1.PageIndex = e.NewPageIndex;
        Session["ar_inv_page_index"] = e.NewPageIndex;
        if (Convert.ToInt32(Session["ar_inv_page_index"]) == 0)
        {
            Response.Write("<script>window.location.href='Arinvoice.aspx';</script>");
        }

    }
    protected void GridView1_RowDataBound(object sender, GridViewRowEventArgs e)
    {

        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {

                if (drv.DataView.Table.Columns[i].ColumnName.Equals("TR Date"))
                {
                    //  Response.Write(e.Row.Cells[i + 6].Text.ToString());
                    string[] val = e.Row.Cells[i + 1].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 1].Text = val[0].ToString();
                }
            }
        }
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }
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
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
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

}