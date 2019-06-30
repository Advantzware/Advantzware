using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Linq;

/// <summary>
/// Summary description for viewItem
/// </summary>
public partial class viewItem : System.Web.UI.Page
{
    private string stritem = "";
    private string stext = "";
    public viewItem()
    {
        // TODO: Add constructor logic here
        //
    }
    protected void Page_PreRender(object sender, EventArgs e)
    {
        try
        {
            if (Session["view_order_entry_pages_with_estimate"] != null)
            {
                /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
                ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
                ImageButton img3 = (ImageButton)Master.FindControl("viewitem");                

                img1.Visible = false;
                img2.Visible = false;
                img3.Visible = false;

                ImageButton listitem = (ImageButton)Master.FindControl("ImageButton6");
                listitem.ImageUrl = "~/Images/lisl item 1.jpg";*/
                HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
                HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
                HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
                img1.Attributes.Add("style", "display:none");
                img2.Attributes.Add("style", "display:none");
                img3.Attributes.Add("style", "display:none");
                
                

            }

            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                /*ImageButton img4 = (ImageButton)Master.FindControl("ImageButton4");
                ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
                ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
                ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
                img4.Visible = false;
                img5.Visible = false;
                img6.Visible = false;
                img7.Visible = false;*/
                HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton4");
                HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
                HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
                HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
                img4.Attributes.Add("style", "display:none");
                img5.Attributes.Add("style", "display:none");
                img6.Attributes.Add("style", "display:none");
                img7.Attributes.Add("style", "display:none");

                Btn_Add.Visible = false;
                Btn_Update.Visible = false;
                Btn_View.Visible = false;
                Btn_Delete.Visible = false;
                History_Button.Visible = false;
                Button_Stat.Visible = false;
                price_button.Visible = false;

                Image ack = (Image)Master.FindControl("Image2");
                ImageButton add = (ImageButton)Master.FindControl("img_btn_add");
                Image ticket = (Image)Master.FindControl("Image6");
                //ack.Visible = false;
                add.Visible = false;
                ticket.Visible = false;
            }
        }
        catch { }
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        Image move_col = (Image)Master.FindControl("Image5");
        move_col.Visible = false;
                
        Session["genestno"] = null;
        Session["genestcust"] = null;
        Session["prmuomfld"] = null;
        Session["history_list1_index"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "viewitem.aspx";
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
            
            
                price_button.Visible = false;
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from button_maintain where parent = 'order_estimate.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    if (ds.Tables[0].Rows.Count == 0)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,btn10,chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10) values ('order_estimate.aspx','Order Entry & Order Status','View Order','List Item','View Item','Misc Chgs','Job Status','Releases','Order Total','Item Status','Invoices','Ship Notes','True','True','True','True','True','True','True','True','True','True')", conn);
                        cmd_insert.ExecuteNonQuery();
                    }

                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        if (Session["view_order_entry_pages_with_estimate"] == null)
                        {
                            //ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
                            //ImageButton img3 = (ImageButton)Master.FindControl("viewitem");
                            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
                            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
                            
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img2.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img2.Attributes.Add("style", "display:none");
                            }                            
                            if (ss3.Contains(UserLogin.UserName))
                            {
                               //img3.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img3.Attributes.Add("style", "display:none");
                            }
                            
                            
                        }

                        if (Session["view_order_entry_pages_with_estimate"] != null)
                        {

                            /*ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
                            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
                            ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");*/
                            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
                            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
                            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss2 = dr["user2"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img5.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img5.Attributes.Add("style", "display:none");
                            }
                            if (ss2.Contains(UserLogin.UserName))
                            {
                                //img6.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                                if (dr["chk2"].ToString() == "False")
                                    img6.Attributes.Add("style", "display:none");
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                //img7.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img7.Attributes.Add("style", "display:none");
                            }
                                                        
                        }

                    }
                    conn.Close();


                }
                catch { }
            
        }
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "View Item";

        btnbrowsorder.Visible = true;
        btnonorder.Visible = true;
        returnestimate(sender, e);

        try
        {
            /*ImageButton viewitem = (ImageButton)Master.FindControl("viewitem");
            viewitem.ImageUrl = "~/img/viewitem1.jpg";*/
            int noofrows = 10;
            if (Convert.ToInt32(Session["count"]) > noofrows)
            {
                GridView1.PageSize = Convert.ToInt32(Session["count"]);
            }
            else
            {
                GridView1.PageSize = 50;
            }

            if (Session["view_item_index_val"] == null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["view_item_index_val"]);
                Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
                Session["order_entry_est_no"] = GridView1.SelectedRow.Cells[8].Text;
                Label itemname = (Label)GridView1.SelectedRow.FindControl("Label2");
                //Response.Write(itemname.Text);
                Session["item"] = itemname.Text;
            }

            if (Session["view_item_index_val"] == null)
            {                
                GridView1.SelectedIndex = 0;
                Session["line"] = GridView1.SelectedRow.Cells[1].Text;

                Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
                Session["order_entry_est_no"] = GridView1.SelectedRow.Cells[8].Text;

                Session["order"] = Session["order"];
                Session["order_est"] = Session["order"];
                Session["view_line_est"] = Session["line"];
                
            }
        }
        catch { return; }


        int rows = GridView1.Rows.Count;
        Session["rows"] = rows;
        if (!Page.IsPostBack)
        {
            try
            {
                HiddenField1.Value = Session["line"].ToString();                
                GridView1.SelectedIndex = Convert.ToInt32(HiddenField1.Value) - 1;
                //Response.Write(Session["line"]);
                
            }
            catch { return; }
            //string ss = Session["order"].ToString();
            //FormView1.DataBind();  
            //GridView1.SelectedIndex = Convert.ToInt32(Session["item"]);
        }

    }

    protected void returnestimate(object sender, EventArgs e)
    {
        string estimate = Convert.ToString(Session["orderestimate121"]);
        if (estimate == "&nbsp;")
        {
            btnonorder.Visible = false;
        }
        else
        {
            btnbrowsorder.Visible = false;
        }
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        stritem = this.GridView1.SelectedValue.ToString();
        this.FormView1.DataBind();
        this.FormView2.DataBind();
        this.FormView3.DataBind();

        Session["view_item_index_val"] = GridView1.SelectedIndex;
        Session["order_est_value_check"] = 1;
        Session["line"] = GridView1.SelectedRow.Cells[1].Text;
        Session["order"] = Session["order"];
        Session["order_est"] = Session["order"];
        Session["view_line_est"] = Session["line"];
        Session["orderestimate121"] = GridView1.SelectedRow.Cells[8].Text;
        Session["order_entry_est_no"] = GridView1.SelectedRow.Cells[8].Text;
        Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
        foreach (GridViewRow gvr in GridView1.Rows)
        {

            Label itemname = (Label)GridView1.SelectedRow.FindControl("Label2");
            //Response.Write(itemname.Text);
            Session["item"] = itemname.Text;
        }
        Session["val"] = Session["line"];
        Session["index2"] = Convert.ToInt32(Session["val"]) - 1;
        Page_Load(sender, e);
    }

    //  protected void ObjectDataSource3_Selecting(object sender, ObjectDataSourceSelectingEventArgs e)
    // {
    //     e.InputParameters["prmAction"] = "Select";
    //     e.InputParameters["prmItemNum"] = stritem;
    //  }




    protected void lnkonhand_Click(object sender, EventArgs e)
    {

        Label onhand = (Label)FormView1.FindControl("q_onhLabel");
        Int32 a = Convert.ToInt32(onhand.Text);

        if (a != 0)
        {
            if (Session["order"] != null)
            {
                Session["bin"] = Session["order"];
                Session["fgitem"] = Session["order"];
                Response.Redirect("binjobs.aspx");
            }
        }

    }



    protected void lnkonorder_Click(object sender, EventArgs e)
    {
        Label onorder = (Label)FormView2.FindControl("q_onoLabel");
        Int32 a = Convert.ToInt32(onorder.Text);
        Session["show"] = 1;
        if (a != 0)
        {
            if (Session["order"] != null)
            {
                Session["jobprod"] = Session["order"];
                Session["item"] = Session["item"];
                Response.Redirect("brwsjobs.aspx");
            }
        }
    }
    protected void lnk_allocated_Click(object sender, EventArgs e)
    {
        Label allocated = (Label)FormView3.FindControl("q_allocLabel");
        Int32 a = Convert.ToInt32(allocated.Text);

        if (a != 0)
        {
            if (Session["item"] != null)
            {
                Session["fgitem1"] = Session["item"];
                Response.Redirect("order_inquiry.aspx");
            }
        }
    }
    protected void lnkbrowsorder_Click(object sender, EventArgs e)
    {
        Label onorder = (Label)FormView2.FindControl("q_onoLabel");
        Int32 a = Convert.ToInt32(onorder.Text);

        if (a != 0)
        {
            if (Session["order"] != null)
            {
                Session["BrowsePO"] = Session["order"];
                Session["item"] = Session["item"];

                Response.Redirect("browsepo.aspx");
            }
        }
    }
    protected void Btn_Delete_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        

        orderentry ordentry = new orderentry();
        ordentry.SelectViewItemEstimate(UserLogin.UserName, "Delete", Convert.ToInt32(Session["order_est"]), Convert.ToInt32(Session["view_line_est"]), "", "", "", 0, "", "", "", "", 0, "", "", "", "", 0, 0, "", Convert.ToDateTime("01/01/2000"), 0, "", Convert.ToDateTime("01/01/2000"), 0, 0, 0, 0, 0, 0, "", "", "", "", 0, 0, "", "", "", "", "", "", 0, 0, 0, 0, 0, 0, 0, 0, "");
        GridView1.DataBind();
        Session["view_item_index_val"] = null;
    }
    protected void Btn_View_Click(object sender, EventArgs e)
    {
        Session["view_item_mode"] = null;
        Response.Redirect("view_item_estimate.aspx");
    }
    protected void Btn_Add_Click(object sender, EventArgs e)
    {
        Session["view_item_mode"] = "add";
        Response.Redirect("view_item_estimate.aspx");
    }
    protected void Btn_Update_Click(object sender, EventArgs e)
    {
        Session["view_item_mode"] = "edit";
        Response.Redirect("view_item_estimate.aspx");
    }

   
}
