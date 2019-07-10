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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Data.SqlClient;

public partial class Order_inq : System.Web.UI.Page
{
    string raw_url;
    private string strItem = "";
    private string rbtext = "";
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";

    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
    }
    protected void Page_Load(object sender, EventArgs e)
    {        
        Session["order_est_page_index"] = null;
        Session["top_spec_list_notes_index"] = null;
        Session["spec_list_notes_index"] = null;
        Session["order_list_notes_index"] = null;

        Session["Item_Inquiry_value_check"] = null;
        Session["view_order_entry_pages"] = null;
        Session["view_order_entry_pages_with_estimate"] = null;
        Session["order_entry_status_check"] = null;
        Session["top_list_attach_rec_key"] = null;
        Session["item_show_hide_button"] = null;

        Session["view_order_inquiry_pages"] = "5";
        Image ticket = (Image)Master.FindControl("Image6");
        ticket.Visible = false;
        try
        {
             
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });           
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();                       
            string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
            Session["item"] = selectedText2;
        }
        catch { }
        
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton4");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");

            /*ImageButton img4 = (ImageButton)Master.FindControl("ImageButton4");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
            ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;
            img7.Visible = false;*/

            ImageButton add = (ImageButton)Master.FindControl("img_btn_add");
            Image ack = (Image)Master.FindControl("Image2");
            add.Visible = false;
            ack.Visible = false;
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");                       

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;*/

            ImageButton img4 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;
            img11.Visible = false;
        }

        Session["show"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (!Page.IsPostBack)
        {            
            ddl_po_status.SelectedIndex = 2;


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_inquiry.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    if (dr["rd_field1"].ToString() == "Any")
                        ddl_po_status.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Open")
                        ddl_po_status.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "Pending")
                        ddl_po_status.SelectedIndex = 2;
                    if (dr["rd_field1"].ToString() == "Closed")
                        ddl_po_status.SelectedIndex = 3;
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }

                
            if (ddl_po_status.SelectedIndex == 0)
            {
                Session["ddl_po_status_value"] = "OPEN";
            }
            if (ddl_po_status.SelectedIndex == 1)
            {
                Session["ddl_po_status_value"] = "ANY";
            }
            if (ddl_po_status.SelectedIndex == 2)
            {
                Session["ddl_po_status_value"] = "PENDING";
            }
            if (ddl_po_status.SelectedIndex == 3)
            {
                Session["ddl_po_status_value"] = "CLOSED";
            }
            try
            {
                Session["Rowuser"] = UserLogin.UserName;
                TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
                GridView2.PageSize = Convert.ToInt32(ddl_display.Text);
            }
            catch { }
        }
                
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_inquiry.aspx";
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
                //customerid.Visible = false;
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
        }

        
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Brws Order";
        ddl_order.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customer.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customerpo.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job1.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_quote.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        try
        {
            TextBox rows_per_page = (TextBox)FormView1.FindControl("aLineLabel");
            rows_per_page.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch { }
        /*ImageButton brwsorder = (ImageButton)Master.FindControl("brwsorder");
        brwsorder.ImageUrl = "~/img/brwsorder1.jpg";*/

        Session["count"] = GridView2.Rows.Count;

        if (Session["fgitem1"] != null)
        {
            txt_fgitem.Text = Convert.ToString(Session["fgitem1"]);
            Session["prmFgitem"] = txt_fgitem.Text;
            
            Session["prmCust"] = txt_customer.Text.Trim();
            Session["prmAction"] = "Search";
            Session["prmPonum"] = txt_customerpo.Text.Trim();
            Session["prmPartno"] = txt_custpart.Text.Trim();
            Session["prmPostatus"] = ddl_po_status.SelectedValue.Trim();
            Session["prmOrderNum"] = ddl_order.Text.Trim();
            Session["prmFgitem"] = txt_fgitem.Text.Trim();
            Session["prmEst"] = txt_estimate.Text.Trim();
            Session["prmJob"] = txt_job1.Text.Trim();
            Session["prmJob2"] = txt_job2.Text.Trim();
            Session["prmQuote"] = txt_quote.Text.Trim();
                        
            GridView2.SelectedIndex = 0;            
            Session["prmFgitem"] = null;
            Session["index"] = null;
            Session["order"] = null;

        }
        if (Session["order"] != null)
        {
            Session["prmOrderNum"] = Session["temp"];
        }

        try
        {
            if (Session["index"] == null)
            {
                
                GridView2.SelectedIndex = 0;
                Session["order"] = ((Label)GridView2.SelectedRow.FindControl("order_label")).Text;
                Session["customer_fglookup_val"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
                Session["item"] = selectedText2;
                Session["line"] = 1;                
                Session["orderestimate121"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;                
                Session["cust"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;                
                Session["order_inquiry_status_check"] = ((Label)GridView2.SelectedRow.FindControl("status_label")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
               
            }
        }
        catch
        {
            return;
        }
        
        int rows = GridView2.Rows.Count;
        if (rows == Convert.ToInt32(Session["rows"]))
        {
            if (Session["index2"] != null)
            {
                Session["index"] = Session["index2"];
                GridView2.SelectedIndex = Convert.ToInt32(Session["index"]);
                Session["index2"] = null;
            }
        }        
        GridView2.SelectedIndex = Convert.ToInt32(Session["index"]);
        if (Session["index"] != null)
        {
            try
            {
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
                string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
                Session["item"] = selectedText2;
            }
            catch { }

        }
        if (Session["order"] == null)
        {            
            GridView2.SelectedIndex = -1;
        }
        try
        {
            if (Session["gridsize"] != null)
            {
               
                GridView2.PageSize = Convert.ToInt32(Session["gridsize"]);
            }

            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");            
            Session["size"] = Convert.ToInt32(ddl_display.Text);
           
            GridView2.PageSize = Convert.ToInt32(Session["size"]);
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

        if (Session["prmCust"] == null || Convert.ToString(Session["prmCust"]) == "")
            Session["prmCust"] = "";

        if (Session["prmPonum"] == null || Convert.ToString(Session["prmPonum"]) == "")
            Session["prmPonum"] = "";
        if (Session["prmPartno"] == null || Convert.ToString(Session["prmPartno"]) == "")
            Session["prmPartno"] = "";
        if (Session["prmPostatus"] == null || Convert.ToString(Session["prmPostatus"]) == "")
            Session["prmPostatus"] = /*"open";*/ Session["ddl_po_status_value"];
        if (Session["prmOrderNum"] == null || Convert.ToString(Session["prmOrderNum"]) == "")
            Session["prmOrderNum"] = 0;
        if (Session["prmFgitem"] == null || Convert.ToString(Session["prmFgitem"]) == "")
            Session["prmFgitem"] = "";        
        if (Session["prmEst"] == null || Convert.ToString(Session["prmEst"]) == "")
            Session["prmEst"] = "";
        if (Session["prmJob"] == null || Convert.ToString(Session["prmJob"]) == "")
            Session["prmJob"] = "";
        if (Session["prmJob2"] == null || Convert.ToString(Session["prmJob2"]) == "")
            Session["prmJob2"] = "";
        if (Session["prmQuote"] == null || Convert.ToString(Session["prmQuote"]) == "")
            Session["prmQuote"] = "";

        if (Convert.ToString(Session["prmAction"]) != "Search" || Convert.ToString(Convert.ToString(Session["prmAction"])) == "" || Session["prmAction"] == null)
        {
            Session["prmAction"] = "Select";
        }
        if (Session["fgitem1"] != null)
        {
            Session["prmFgitem"] = Session["fgitem1"];
        }
        Order entry = new Order();        
        DataSet ds_entry = new DataSet();
        ds_entry = entry.SelectOrder(UserLogin.UserName, Convert.ToString(Session["prmCust"]), Convert.ToString(Session["prmAction"]), Convert.ToString(Session["prmPonum"]), Convert.ToString(Session["prmPartno"]), Convert.ToString(Session["prmPostatus"]), Convert.ToString(Session["prmOrderNum"]), Convert.ToString(Session["prmFgitem"]), Convert.ToString(Session["prmEst"]), Convert.ToString(Session["prmJob"]), Convert.ToString(Session["prmJob2"]), Convert.ToString(Session["prmQuote"]));
      
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "order_inquiry.aspx" + "' and sub_program = '" + "order_inquiry.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            
            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "order_inquiry.aspx" + "' and sub_program = '" + "order_inquiry.aspx" + "' and display = 0";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);

            
            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["ord_inq_initial_val"] = ds2.Tables[0].Rows.Count;

                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());
                   
                    if (ds2.Tables[0].Rows[0][0].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }

                }
                ds_entry.Tables[0].Columns["prom-code"].SetOrdinal(26);
                ds_entry.Tables[0].Columns["Line"].SetOrdinal(27);
                for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
                {
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Name";
                    }

                    if (ds_entry.Tables[0].Columns[i].ColumnName == "req-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Due Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Sell Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "pr-uom")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Uom";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "t-price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Extended Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item Name";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "part-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Part";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "po-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer PO#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vEst")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Est#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "q-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Quote#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob2")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "stat")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Status";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "qty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Ordered Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "Prod")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Prod Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vshipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Shipped Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vinvqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Invoice Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "onhandqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "On Hand Quantity";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "actrelqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Act Rel Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "wipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Wip Qty/Balance Due";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "oupct")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "O/U%";
                    }
                }

                DataView dv = ds_entry.Tables[0].DefaultView;
                
                if (Session["order_inq_gridview2_sort"] == null)
                {
                    dv.Sort = "Order DESC";
                }
                if (Session["order_inq_gridview2_sort"] != null)
                {
                    dv.Sort = Convert.ToString(Session["order_inq_gridview2_sort"]);
                }
                GridView2.DataSource = dv;
                GridView2.DataBind();
            }
            else
            {
                
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "order_inquiry.aspx" + "' and sub_program = '" + "order_inquiry.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["ord_inq_initial_val"] = mainds.Tables[0].Rows.Count;
                
                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                    
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }

                }
                ds_entry.Tables[0].Columns["prom-code"].SetOrdinal(26);
                ds_entry.Tables[0].Columns["Line"].SetOrdinal(27);
                for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
                {
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Name";
                    }

                    if (ds_entry.Tables[0].Columns[i].ColumnName == "req-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Due Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Sell Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "pr-uom")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Uom";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "t-price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Extended Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item Name";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "part-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Part";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "po-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer PO#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vEst")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Est#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "q-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Quote#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob2")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "stat")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Status";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "qty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Ordered Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "Prod")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Prod Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vshipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Shipped Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vinvqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Invoice Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "onhandqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "On Hand Quantity";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "actrelqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Act Rel Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "wipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Wip Qty/Balance Due";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "oupct")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "O/U%";
                    }
                }
                DataView dv = ds_entry.Tables[0].DefaultView;
                if (Session["order_inq_gridview2_sort"] == null)
                {
                    dv.Sort = "Order DESC";
                }
                if (Session["order_inq_gridview2_sort"] != null)
                {
                    dv.Sort = Convert.ToString(Session["order_inq_gridview2_sort"]);
                }
                GridView2.DataSource = dv;
                GridView2.DataBind();
            }           
        }
        catch { }
        Session["fgitem1"] = null;
      
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
       
        Session["prmCust"] = txt_customer.Text.Trim();
        Session["prmAction"] = "Search";
        Session["prmPonum"] = txt_customerpo.Text.Trim();
        Session["prmPartno"] = txt_custpart.Text.Trim();
        Session["prmPostatus"] = ddl_po_status.SelectedValue.Trim();
        Session["prmOrderNum"] = ddl_order.Text.Trim();
        Session["prmFgitem"] = txt_fgitem.Text.Trim();
        Session["prmEst"] = txt_estimate.Text.Trim();
        Session["prmJob"] = txt_job1.Text.Trim();
        Session["prmJob2"] = txt_job2.Text.Trim();
        Session["prmQuote"] = txt_quote.Text.Trim();
        Session["temp"] = ddl_order.Text;
        Session["index"] = null;
        Session["order"] = null;
        Session["item"] = null;
        Session["line"] = null;
        Session["index"] = null;
        Session["index2"] = null;
        Session["order_inquiry_status_check"] = null;
       
        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_inquiry.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , rd_field1) values ('" + UserLogin.UserName + "','order_inquiry.aspx' , '" + ddl_po_status.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set rd_field1 = '" + ddl_po_status.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'order_inquiry.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {            
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

        Page_Load(sender, e);
    }



    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        ddl_order.Text = str.ToString();
        txt_customer.Text = str.ToString();
        txt_customerpo.Text = str.ToString();
        txt_custpart.Text = str.ToString();
        txt_estimate.Text = str.ToString();
        txt_fgitem.Text = str.ToString();
        txt_job1.Text = str.ToString();
        txt_job2.Text = str.ToString();
        txt_quote.Text = str.ToString();
        ddl_po_status.SelectedIndex = 1;

        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmCust"] = txt_customer.Text.Trim();
        Session["prmAction"] = "Select";
        Session["prmPonum"] = txt_customerpo.Text.Trim();
        Session["prmPartno"] = txt_custpart.Text.Trim();
        Session["prmPostatus"] = ddl_po_status.SelectedValue.Trim();
        Session["prmOrderNum"] = ddl_order.Text.Trim();
        Session["prmFgitem"] = txt_fgitem.Text.Trim();
        Session["prmEst"] = txt_estimate.Text.Trim();
        Session["prmJob"] = txt_job1.Text.Trim();
        Session["prmJob2"] = txt_job2.Text.Trim();
        Session["prmQuote"] = txt_quote.Text.Trim();
        Session["temp"] = null;
        Session["index"] = null;
        Session["order"] = null;
        Session["item"] = null;
        Session["line"] = null;
        Session["index"] = null;
        Session["index2"] = null;
        Session["gridsize"] = null;
        Session["order_inquiry_status_check"] = null;
        Session["order_inq_gridview2_sort"] = null;
       
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_inquiry.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , rd_field1) values ('" + UserLogin.UserName + "','order_inquiry.aspx' , '" + ddl_po_status.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set rd_field1 = '" + ddl_po_status.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'order_inquiry.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

        Page_Load(sender, e);
    }


   
   
    
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;        
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    
    protected void ddl_po_status_SelectedIndexChanged(object sender, EventArgs e)
    {
        if (ddl_po_status.SelectedIndex == 0)
        {
            Session["ddl_po_status_value"] = "OPEN";
        }
        if (ddl_po_status.SelectedIndex == 1)
        {
            Session["ddl_po_status_value"] = "ANY";
        }
        if (ddl_po_status.SelectedIndex == 2)
        {
            Session["ddl_po_status_value"] = "PENDING";
        }
        if (ddl_po_status.SelectedIndex == 3)
        {
            Session["ddl_po_status_value"] = "CLOSED";
        }
        btnSearch_Click(sender, e);
    }

    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {        
        try
        {
            if (Session["ord_inq_initial_val"] != null)
            {                
                for (int show = Convert.ToInt32(Session["ord_inq_initial_val"]) + 7; show < 32; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                e.Row.Cells[32].Visible = false;
                e.Row.Cells[33].Visible = false;
                e.Row.Cells[34].Visible = false;
                e.Row.Cells[35].Visible = false;
                e.Row.Cells[36].Visible = false;
                e.Row.Cells[37].Visible = false;
                e.Row.Cells[38].Visible = false;
                e.Row.Cells[39].Visible = false;
                e.Row.Cells[40].Visible = false;
                e.Row.Cells[41].Visible = false;
                e.Row.Cells[42].Visible = false;
                e.Row.Cells[43].Visible = false;
                e.Row.Cells[44].Visible = false;
                e.Row.Cells[45].Visible = false;
                e.Row.Cells[46].Visible = false;
                e.Row.Cells[47].Visible = false;
                e.Row.Cells[48].Visible = false;
                e.Row.Cells[49].Visible = false;
                e.Row.Cells[50].Visible = false;
                e.Row.Cells[51].Visible = false;
                e.Row.Cells[52].Visible = false;
                e.Row.Cells[53].Visible = false;
                e.Row.Cells[54].Visible = false;


                
            }


        }
        catch { }
    }
    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        GridView2.PageIndex = e.NewPageIndex;
        
    }
    public SortDirection GridViewSortDirection
    {
        get
        {
            if (ViewState["sortDirection"] == null)
                ViewState["sortDirection"] = SortDirection.Ascending;
            return (SortDirection)ViewState["sortDirection"];
        }
        set { ViewState["sortDirection"] = value; }
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

        Session["order_inq_gridview2_sort"] = sortExpression + direction;
        
    }
    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
       
        int index = GridView2.SelectedIndex;
        Session["index"] = index;

        try
        {
            Session["customer_fglookup_val"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
            Session["orderestimate121"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
            Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
            Session["cust"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
            Session["order_inquiry_status_check"] = ((Label)GridView2.SelectedRow.FindControl("status_label")).Text;
            Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
            foreach (GridViewRow gv in GridView2.Rows)
            {
                string selectedText = ((Label)GridView2.SelectedRow.FindControl("value")).Text;
                Session["order"] = ((Label)GridView2.SelectedRow.FindControl("order_label")).Text;
                string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
                Session["item"] = selectedText2;
                Session["line"] = selectedText;
            }
        }
        catch
        {
        }
    }
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;            
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Due Date"))
                {                    
                    string[] val = e.Row.Cells[i+7].Text.Split(new char[] { ' ' });                                        
                    e.Row.Cells[i + 7].Text = val[0].ToString() ;                    
                }
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Order Date"))
                {
                    string[] val = e.Row.Cells[i + 7].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 7].Text = val[0].ToString();
                }
            }            
        }        

        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }

    } 
}
